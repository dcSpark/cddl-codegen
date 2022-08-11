const fs = require('fs');

const inputFile = fs.readFileSync('./rust/wasm/pkg/cddl_lib_wasm.d.ts', 'utf8').split(/\r?\n/);
let currentClass = null;
for (let i = 0; i < inputFile.length; ++i) {
  let line = inputFile[i];
  const classDef = /export class(.*){/.exec(line);
  if (classDef != null && classDef.length > 1) {
    currentClass = classDef[1].trim();
    continue;
  }
  inputFile[i] = line.replace(/(\s?to_js_value\(\)\s?:\s?)(any)(;)/, `$1${currentClass}JSON$3`);
  if (line != inputFile[i]) {
    continue;
  }
  inputFile[i] = line.replace(/(\s?\*\s?\@returns\s\{)(any)(\})/, `$1${currentClass}JSON$3`);
}
const jsonDefs = fs.readFileSync('./rust/json-gen/output/json-types.d.ts', 'utf8');
fs.writeFile(
  './rust/wasm/pkg/cddl_lib_wasm.d.ts',
  `${inputFile.join('\n')}\n${jsonDefs}`,
  (err) => {
    if (err != null) {
      console.log(`err writing file: ${err}`)
    }
  }
);
