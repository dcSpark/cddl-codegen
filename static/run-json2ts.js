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

const schemasDir = path.join(wasmDir, 'json-gen', 'schemas');
if (!fs.existsSync(schemasDir)) {
  console.error(
    `${SCRIPT}: no schema directory at ${schemasDir}. Run the json-gen crate ` +
    `(\`cd ${path.join(wasmDir, 'json-gen')} && cargo run\`) first, which writes the schemas this ` +
    `script compiles.`);
  process.exit(1);
}
const outputFile = path.join(wasmDir, 'json-gen', 'output', 'json-types.d.ts');
const schemaFiles = fs.readdirSync(schemasDir).filter(file => path.extname(file) === '.json');

// Each schema is self-contained: schemars emits every referenced type inline under the file's own
// `$defs`, so json2ts resolves all `#/$defs/...` refs locally — no cross-file `$ref` rewriting is
// needed. With `declareExternallyReferenced: false` (below), a referenced type is declared once by
// its own per-type schema file and only *referenced* in others; the dedup pass after compile
// collapses any duplicate declarations before the JSON-suffix rename.

// A schema that fails to compile is a hard failure of the whole run, never a silently-dropped type:
// a dropped type leaves the merged `.d.ts` referencing a name nothing declares, and the run would
// still have overwritten the last-good output on its way out.
const failures = [];

Promise.all(schemaFiles.map(schemaFile => {
  const completeName = path.join(schemasDir, schemaFile);
  const originalFile = fs.readFileSync(completeName, 'utf8');
  let schemaObj = JSON.parse(originalFile);

  // this gets rid of [k: string]: unknown in generated .ts
  // but we shouldn't do this if it already exists in the case
  // of map types
  if (typeof schemaObj.additionalProperties !== 'object') {
    schemaObj.additionalProperties = false;
  }
  return json2ts.compile(schemaObj, schemaFile, {
    declareExternallyReferenced: false,
    cwd: schemasDir,
    bannerComment: ''
  }).catch(e => {
    failures.push(`  ${schemaFile}: ${e && e.stack ? e.stack : e}`);
    return null;
  });

})).then(tsDefs => {
  if (failures.length > 0) {
    console.error(
      `${SCRIPT}: ${failures.length} of ${schemaFiles.length} schema file(s) failed to compile ` +
      `(schemas dir: ${schemasDir}). ${outputFile} was left untouched.`);
    for (const failure of failures) {
      console.error(failure);
    }
    process.exitCode = 1;
    return;
  }
  const defs = tsDefs.join('').split(/\r?\n/);
  let dedupedDefs = [];
  let start = null;
  let added = new Set();
  const addDef = (cur) => {
    if (start != null) {
      let defName = defs[start].match(/export\s+(type|interface)\s+(\w+).*/);
      let defKey = null;
      if (defName != null && defName.length > 2) {
        defKey = defName[2];
      } else {
        console.error(`${SCRIPT} could not find name for de-dup(${defName != null}): "${defs[start]}"`);
      }
      if (defKey == null || !added.has(defKey)) {
        for (let j = start; j < cur; ++j) {
          dedupedDefs.push(defs[j]);
        }
        if (defKey != null) {
          added.add(defKey);
        }
      }
    }
    start = cur;
  };
  for (let i = 0; i < defs.length; ++i) {
    if (defs[i].startsWith('export')) {
      addDef(i);
    }
  }
  addDef(defs.length);
  // prepend 'JSON' to all identifiers here so they don't conflict with main .ts types
  for (let i = 0; i < dedupedDefs.length; ++i) {
    for (let id of added) {
      dedupedDefs[i] = dedupedDefs[i].replace(new RegExp(`\\b${id}\\b`), id + 'JSON');
    }
  }
  fs.mkdirSync(path.dirname(outputFile), { recursive: true });
  fs.writeFileSync(outputFile, dedupedDefs.join('\n'));
}).catch(e => {
  console.error(`${SCRIPT}: ${e && e.stack ? e.stack : e}`);
  process.exitCode = 1;
});
