const fs = require('fs');
const json2ts = require('json-schema-to-typescript');
const path = require('path');

const SCRIPT = 'run-json2ts.js';

// LOCKSTEP (argument convention + root/wasm-dir resolution): the `--name=value` parser and the
// `resolveRoot`/`resolveWasmDir` pair below are mirrored verbatim in `json-ts-types.js`. The
// generator ships exactly these two self-contained files (`--package-json --json-schema-export`, or
// `--json-schema-scripts` on its own), so neither may `require` the other — a change to one must be
// mirrored into the other in the same commit.
function parseArgs(script, argv, known) {
  const out = {};
  for (const arg of argv) {
    const m = /^--([\w-]+)=(.*)$/.exec(arg);
    if (m == null || !known.includes(m[1])) {
      // A typo'd override must not silently fall back to the default and generate against the
      // wrong tree, so an unrecognized argument is fatal rather than ignored.
      console.error(
        `${script}: unrecognized argument ${JSON.stringify(arg)}. Supported: ` +
        known.map(k => `--${k}=<value>`).join(', '));
      process.exit(1);
    }
    out[m[1]] = m[2];
  }
  return out;
}

// Both shipped layouts place the scripts at `<root>/scripts/*.js`, so the root is derivable from
// the script's own location — no dependence on the caller's cwd.
function resolveRoot(override) {
  return override != null ? path.resolve(override) : path.resolve(__dirname, '..');
}

// `--package-json` nests the crates one level deeper (`<root>/rust/wasm`); the bare
// `--json-schema-scripts` layout puts the wasm crate at `<root>/wasm`.
function resolveWasmDir(script, root, override) {
  if (override != null) {
    const dir = path.resolve(override);
    if (!fs.existsSync(dir)) {
      console.error(`${script}: --wasm-dir=${dir} does not exist`);
      process.exit(1);
    }
    return dir;
  }
  const candidates = [path.join(root, 'rust', 'wasm'), path.join(root, 'wasm')];
  const found = candidates.find(candidate => fs.existsSync(candidate));
  if (found == null) {
    console.error(
      `${script}: could not find the wasm crate directory. Looked for:\n  ` +
      candidates.join('\n  ') +
      `\n(root resolved to ${root}). Pass --wasm-dir=<dir>, or --root=<dir>, to override.`);
    process.exit(1);
  }
  return found;
}
// end LOCKSTEP block

const args = parseArgs(SCRIPT, process.argv.slice(2), ['root', 'wasm-dir']);
const root = resolveRoot(args.root);
const wasmDir = resolveWasmDir(SCRIPT, root, args['wasm-dir']);

// Fail loudly and leave the last-good output alone; never write a partial `.d.ts`.
function fail(message) {
  console.error(`${SCRIPT}: ${message}`);
  process.exit(1);
}

const schemasDir = path.join(wasmDir, 'json-gen', 'schemas');
if (!fs.existsSync(schemasDir)) {
  fail(
    `no schema directory at ${schemasDir}. Run the json-gen crate ` +
    `(\`cd ${path.join(wasmDir, 'json-gen')} && cargo run\`) first, which writes the schema ` +
    `document this script compiles.`);
}
const outputFile = path.join(wasmDir, 'json-gen', 'output', 'json-types.d.ts');

// The json-gen crate writes exactly ONE document per crate — `<crate>.schema.json`, a pure `$defs`
// bundle threaded through a single schemars generator — so every type any other type references is
// a definition in the same file. That is what makes one `compile()` call correct: each definition
// becomes a top-level declaration, and the declared set cannot diverge from the referenced set.
const jsonFiles = fs.readdirSync(schemasDir).filter(file => path.extname(file) === '.json');
const documents = jsonFiles.filter(file => file.endsWith('.schema.json'));
const strays = jsonFiles.filter(file => !file.endsWith('.schema.json'));

if (documents.length !== 1) {
  fail(
    `expected exactly one *.schema.json document in ${schemasDir}, found ${documents.length}` +
    (documents.length > 0 ? `:\n  ${documents.join('\n  ')}` : '') +
    `\nAll .json files present:${jsonFiles.length > 0 ? `\n  ${jsonFiles.join('\n  ')}` : ' (none)'}`);
}
if (strays.length > 0) {
  // A leftover per-type schema from a pre-merge generator run. Ignoring it would keep a deleted
  // type shipping, so it is fatal and the fix is named.
  fail(
    `${strays.length} stale per-type schema file(s) in ${schemasDir}:\n  ${strays.join('\n  ')}\n` +
    `The json-gen crate writes one ${documents[0]} document and never deletes anything, so these ` +
    `are left over from an older generator run. Delete them and re-run the json-gen crate.`);
}

const documentFile = documents[0];
const documentText = fs.readFileSync(path.join(schemasDir, documentFile), 'utf8');
const document = JSON.parse(documentText);
const sourceDefs = document.$defs || document.definitions;
if (sourceDefs == null || Object.keys(sourceDefs).length === 0) {
  // An empty JSON surface must not produce a green build: the wasm classes' `to_json_value()` would
  // silently stay `any` and nothing would say why.
  fail(`${documentFile} has no definitions ($defs). Refusing to write an empty ${outputFile}.`);
}

// `JSON` suffix so these names cannot collide with the wasm classes they describe. This is the name
// the emitted declaration MUST carry — `json-ts-types.js` keys the splice on it — so how it gets
// there is the subject of the next block: the naive route (compile under the real name, rename
// afterwards if json2ts mangled it) is unavailable, because renaming an identifier in the output
// also rewrites matching words inside doc comments and string-literal unions.
const suffixed = (name) => `${name}JSON`;

// The document needs a root that references every definition, or json2ts prunes the ones nothing
// else points at (a schema root that no other type embeds — a large fraction of a real corpus).
// Stripped from the output again below.
const ROOT_TITLE = '__AllSchemas';

// json2ts does not emit a title verbatim: it runs the name through its own identifier
// normalization, which is NOT the identity on every valid identifier (it uppercases a letter
// following a digit, so `Blake2b256JSON` is published as `Blake2B256JSON`). `json-ts-types.js`
// keys the splice on the exact `<Class>JSON` spelling, so a normalized name reads to it as a class
// the document published no type for — the class ships `any` and the consumer's only remedy would
// be to rename their CDDL rule.
//
// So the normalization is removed from the contract rather than modelled: each definition compiles
// under a SYNTHETIC title that the normalization provably leaves alone, and every occurrence of
// that token in the emitted TypeScript is mapped back to `<key>JSON` afterwards. The guarantee is
// then exact — the declaration name is `<key>JSON` for every key it can be — and it does not
// depend on json2ts's naming rules staying what they are today.
//
// Two properties make the map-back safe. Both are established here, not assumed:
//
//   * FIXED POINT. The token is ASCII letters only, with an uppercase first letter: no digits, no
//     `$`, no underscores, no accented letters and no separators — those are the only things
//     json2ts's normalization rewrites. So it emits the token verbatim and `<key>JSON` lands
//     exactly where the token was. (Digit-letter adjacency is what mangles `Blake2b256`, so the
//     index below is spelled in letters rather than digits.)
//   * ABSENCE. The token does not occur anywhere in the source document's text. This is the ENTIRE
//     safety argument for replacing text after the compile — which is otherwise exactly what this
//     script avoids, by setting titles before compiling: renaming an identifier in json2ts's
//     output also rewrites matching words inside doc comments and string-literal unions, both of
//     which carry text straight from the document. A token absent from the source cannot BE one of
//     those words, so every occurrence in the output is a name json2ts minted from our title.
//
// Scoped to keys where `<key>JSON` is a valid TypeScript identifier: a `$defs` key need not be one
// (the static runtime publishes `OrderedHashMap<K, V>`), and there is no name to guarantee when
// the target cannot be spelled. Those keys keep the plain suffixed title and json2ts normalizes it
// as before — no wasm class can carry such a name, so `json-ts-types.js` never keys on one.
const TS_IDENTIFIER = /^[A-Za-z_$][\w$]*$/;
const SYNTHETIC_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
const synthesizable = Object.keys(sourceDefs).filter((name) => TS_IDENTIFIER.test(suffixed(name)));
// Lengthening (rather than, say, a random suffix) keeps the absence proof deterministic: the same
// document always picks the same prefix, and reproducible output is a hard requirement here.
let syntheticPrefix = 'CddlCodegenSynthDef';
while (documentText.includes(syntheticPrefix) || JSON.stringify(document).includes(syntheticPrefix)) {
  syntheticPrefix += 'X';
}
// Fixed width, so no token is a prefix of another and the map-back cannot mis-split one.
let syntheticWidth = 1;
while (Math.pow(26, syntheticWidth) < synthesizable.length) syntheticWidth += 1;
const syntheticToken = (index) => {
  let letters = '';
  for (let i = 0, rest = index; i < syntheticWidth; ++i, rest = Math.floor(rest / 26)) {
    letters = SYNTHETIC_ALPHABET[rest % 26] + letters;
  }
  return `${syntheticPrefix}${letters}`;
};

// `$defs` key -> the key this script compiles it under, and (for the synthetic ones) back again.
const emittedKey = new Map();
const mapBack = new Map();
let syntheticIndex = 0;
for (const name of Object.keys(sourceDefs)) {
  const target = suffixed(name);
  if (!TS_IDENTIFIER.test(target)) {
    emittedKey.set(name, target);
    continue;
  }
  const token = syntheticToken(syntheticIndex++);
  emittedKey.set(name, token);
  mapBack.set(token, target);
}

// Annotations sitting beside a `$ref` are legal in 2020-12, but json2ts reads the combination as a
// schema distinct from its target and emits a near-duplicate type (`FooJSON1`). They are pure
// documentation, so drop them and keep the reference bare. Known cost, accepted deliberately: a
// field-level doc comment on a `$ref`-typed field does not reach the emitted TypeScript.
const ANNOTATION_KEYWORDS =
  ['description', 'title', 'default', 'examples', 'deprecated', 'readOnly', 'writeOnly'];

const knownNames = new Set(Object.keys(sourceDefs));
const rewriteRefs = (node) => {
  if (Array.isArray(node)) {
    node.forEach(rewriteRefs);
    return;
  }
  if (node === null || typeof node !== 'object') return;
  if (typeof node.$ref === 'string') {
    const bare = node.$ref.replace(/^#\/(?:\$defs|definitions)\//, '');
    if (knownNames.has(bare)) {
      node.$ref = `#/$defs/${emittedKey.get(bare)}`;
    }
    for (const keyword of ANNOTATION_KEYWORDS) delete node[keyword];
  }
  for (const [key, value] of Object.entries(node)) {
    if (key !== '$ref') rewriteRefs(value);
  }
};
// Ref rewriting runs BEFORE the per-definition title is written, so the annotation strip above can
// never delete the title this script is about to set.
rewriteRefs(sourceDefs);

const defs = Object.create(null);
for (const [name, body] of Object.entries(sourceDefs)) {
  // json2ts names a type from its `title` in preference to its `$defs` key, so the title has to
  // carry the compiled-under name too — otherwise a definition that arrived with a title (from a
  // Rust doc comment, say) emits an unsuffixed name that merges with the wasm class of the same
  // name.
  const def = { ...body, title: emittedKey.get(name) };
  // Suppresses `[k: string]: unknown` on the emitted type, unless the definition really is a map.
  // Per-definition, because every definition becomes a top-level declaration here.
  if (typeof def.additionalProperties !== 'object') def.additionalProperties = false;
  defs[emittedKey.get(name)] = def;
}

// TypeScript requires every named property to be assignable to the type's index signature, which
// is what json2ts renders a catch-all — `patternProperties` or `additionalProperties` — as. A schema
// declaring named keys BESIDE a catch-all, the shape a CDDL open-map rest row (`* k => v`) flattened
// into a record produces, is valid JSON Schema, since the two key spaces are disjoint (a catch-all
// ranges over exactly the members the named keys do not match), but has no exact TypeScript
// equivalent: json2ts emits the named properties beside the index signature and every one of them
// fails TS2411 against it. Widen each catch-all to also accept the named property types: the emitted
// declaration is legal and loses only the disjointness, which is the least-false legal TypeScript
// available. Recursive, because the combination is just as expressible (and just as uncompilable) on
// a nested object schema.
//
// This is a PROJECTION-only transform on an in-memory copy — the published schema document keeps the
// exact `additionalProperties`, because widening THAT would make the document over-accept: a rest
// member matching a declared member's schema would validate while `from_json` rejects it. Which key
// domains reach which keyword is the generator's business (a `uint` domain has a string pattern and
// arrives as `patternProperties`; a `text` or typed domain has none and arrives as
// `additionalProperties`), and both spellings project through here identically.
//
// An OPTIONAL named property needs one member more than its own schema. Under `strictNullChecks`
// the type checked against the index signature for `a?: T` is `T | undefined`, so the named schemas
// alone still leave TS2411 on `? foo: uint` beside a rest row. `tsType` is json2ts's escape hatch
// for emitting a raw TypeScript type verbatim, and is what puts `undefined` in the union. A
// property counts as optional unless `required` positively lists it, since `required` may be
// absent, not an array, or name keys that `properties` does not have.
//
// Two catch-alls are left exactly as they are, so the common rest rows keep projecting to the same
// bytes they always have. One that admits every value (`true`, or the empty schema `{}` an `* any =>
// any` row emits) already renders as an `unknown` index signature every named property satisfies —
// widening it would only spell `unknown` more noisily. And a named schema STRUCTURALLY equal to the
// catch-all (a `* uint => uint` row beside a `uint` field) contributes nothing to the union, so the
// members are deduplicated against the catch-all and against each other; when that empties the
// member list the node is not rewritten at all.
//
// The wrap decision must only ever be evaluated on a node that IS a schema. The recursion stays
// generic (as `rewriteRefs` above is), but two keyword classes are handled by name so that every
// node it reaches is one. A schema MAP (`properties`, `patternProperties`, `$defs`, `definitions`,
// `dependentSchemas`, `dependencies`) is entered by its VALUES — the map itself is not a schema, and
// a consumer whose record has fields literally named `properties` and `patternProperties` would
// otherwise have that map satisfy the wrap condition and be silently rewritten. A data-bearing
// keyword (`enum`, `const`, `default`, `examples`) holds instance values, never schemas, so the walk
// stops there rather than treating an object literal as a schema. Everything else is a schema, an
// array of schemas, or a primitive that falls out at the top of the recursion — so no
// schema-position flag is threaded: by construction there is no other way in.
//
// The wrap runs AFTER the node's children are walked, so the `anyOf` built here — which holds the
// very objects `node.properties` holds — is never re-entered. That is what keeps a nested
// named-beside-pattern property from being wrapped a second time through its parent's catch-all.
const SCHEMA_MAP_KEYWORDS =
  ['properties', 'patternProperties', '$defs', 'definitions', 'dependentSchemas', 'dependencies'];
const DATA_KEYWORDS = ['enum', 'const', 'default', 'examples'];
// Structural identity for the dedup above. Key order carries no meaning in a schema, so the object
// keys are sorted on the way out; `JSON.stringify` then serializes each object in that order.
const schemaKey = (schema) => JSON.stringify(schema, (_, value) =>
  value !== null && typeof value === 'object' && !Array.isArray(value)
    ? Object.fromEntries(Object.entries(value).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)))
    : value);
const widenCatchAlls = (node) => {
  if (Array.isArray(node)) {
    node.forEach(widenCatchAlls);
    return;
  }
  if (node === null || typeof node !== 'object') return;
  for (const [key, value] of Object.entries(node)) {
    if (DATA_KEYWORDS.includes(key)) continue;
    if (SCHEMA_MAP_KEYWORDS.includes(key)) {
      if (value !== null && typeof value === 'object' && !Array.isArray(value)) {
        Object.values(value).forEach(widenCatchAlls);
      }
    } else {
      widenCatchAlls(value);
    }
  }
  const named = Object.entries(node.properties || {});
  if (named.length === 0) return;
  const optional =
    named.some(([key]) => !Array.isArray(node.required) || !node.required.includes(key));
  const widen = (owner, key) => {
    const catchAll = owner[key];
    if (catchAll === null || typeof catchAll !== 'object' || Array.isArray(catchAll)) return;
    if (Object.keys(catchAll).length === 0) return;
    const seen = new Set([schemaKey(catchAll)]);
    const members = [];
    for (const [, schema] of named) {
      const identity = schemaKey(schema);
      if (seen.has(identity)) continue;
      seen.add(identity);
      members.push(schema);
    }
    if (optional) members.push({ tsType: 'undefined' });
    if (members.length === 0) return;
    owner[key] = { anyOf: [catchAll, ...members] };
  };
  const patterns = node.patternProperties;
  if (patterns !== null && typeof patterns === 'object' && !Array.isArray(patterns)) {
    for (const pattern of Object.keys(patterns)) widen(patterns, pattern);
  }
  widen(node, 'additionalProperties');
};
// `defs` is a map OF schemas, not a schema, so each definition is what enters the walk.
Object.values(defs).forEach(widenCatchAlls);

const merged = {
  $schema: document.$schema || 'https://json-schema.org/draft/2020-12/schema',
  title: ROOT_TITLE,
  type: 'object',
  additionalProperties: false,
  properties: Object.fromEntries(
    Object.keys(defs).map((name) => [name, { $ref: `#/$defs/${name}` }])
  ),
  $defs: defs,
};

json2ts
  .compile(merged, ROOT_TITLE, { bannerComment: '', cwd: schemasDir })
  .then((ts) => {
    // Drop the synthetic root, keeping every real declaration.
    const withoutRoot = ts.replace(
      new RegExp(`^export interface ${ROOT_TITLE} \\{[\\s\\S]*?\\n\\}\\n?`, 'm'),
      ''
    );
    if (withoutRoot.includes(ROOT_TITLE)) {
      throw new Error(`${ROOT_TITLE} survived into the output; refusing to write it`);
    }
    // Map the synthetic titles back to `<key>JSON`. Safe by the absence property established above:
    // every occurrence of the prefix in this text is a name json2ts minted from one of our titles.
    const mapped = withoutRoot.replace(
      new RegExp(`${syntheticPrefix}[A-Z]{${syntheticWidth}}`, 'g'),
      (token) => mapBack.get(token) || token
    );
    // Same guard class as `ROOT_TITLE`'s: a token that reached the output through some route the
    // map does not know about would ship a name no consumer can have asked for.
    if (mapped.includes(syntheticPrefix)) {
      throw new Error(
        `a synthetic definition title (${syntheticPrefix}...) survived into the output; ` +
        `refusing to write it`);
    }
    // Duplicate declarations cannot be left to the consumer's type-checker: TypeScript MERGES two
    // `interface`s of the same name silently, so a collision would ship a type that is the union of
    // two unrelated shapes with nothing said. Reachable whenever a key whose `<key>JSON` is exact
    // and a key json2ts normalizes land on the same identifier (`OrderedHashMapKV` beside
    // `OrderedHashMap<K, V>`).
    const declarations =
      [...mapped.matchAll(/^export (?:type|interface) ([A-Za-z_$][\w$]*)/gm)].map((m) => m[1]);
    const duplicates =
      [...new Set(declarations.filter((name, i) => declarations.indexOf(name) !== i))].sort();
    if (duplicates.length > 0) {
      throw new Error(
        `${duplicates.length} declaration name(s) are emitted more than once: ` +
        `${duplicates.join(', ')}. TypeScript merges same-named interfaces silently, so this ` +
        `would ship a type nobody declared. Rename the colliding $defs key(s).`);
    }
    if (declarations.length === 0) {
      throw new Error(`no types produced from ${Object.keys(defs).length} definitions`);
    }
    fs.mkdirSync(path.dirname(outputFile), { recursive: true });
    fs.writeFileSync(outputFile, mapped);
    console.log(
      `${SCRIPT}: ${declarations.length} types from ${Object.keys(defs).length} definitions`);
  })
  .catch((e) => {
    // A document that fails to compile is a hard failure of the whole run, never a silently-dropped
    // type: the last-good `.d.ts` stays on disk and the build goes red.
    console.error(
      `${SCRIPT}: ${documentFile} failed to compile (schemas dir: ${schemasDir}). ` +
      `${outputFile} was left untouched.`);
    console.error(`  ${e && e.stack ? e.stack : e}`);
    process.exitCode = 1;
  });
