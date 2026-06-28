// Runs the wasm-pack-built bindings under node and asserts the JSON contract that `cargo build`
// and the snapshot suite structurally cannot see: that `to_json_value()` (serde-wasm-bindgen
// json_compatible) produces the same shape as `JSON.parse(to_json())` (serde_json). Several type
// shapes are exercised (see input.cddl); maps, the externally-tagged enum, and absent/null
// optionals are the cases that come back wrong (JS Map / undefined) under the default serializer.
//
// argv[2] is the wasm-pack `pkg` dir; requiring the directory resolves the right module via the
// package.json "main" it generates, so this stays agnostic to the lib name.
import assert from 'node:assert';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const lib = require(process.argv[2]);

// The invariant is generic, so it doesn't depend on knowing each type's exact JSON shape: whatever
// to_json() emits, to_json_value() must deep-equal it (deepStrictEqual distinguishes Map vs object
// and undefined vs null).
function check(obj, label) {
  assert.deepStrictEqual(
    obj.to_json_value(),
    JSON.parse(obj.to_json()),
    `${label}: to_json_value() != JSON.parse(to_json())`,
  );
}

// Fully populated: non-empty map, nested maps in an array, bytes, present optional, numeric nullable.
check(
  lib.Foo.from_json(
    '{"a_map":{"a":1,"b":2},"a_list":[10,20],"nested":[{"x":1},{"y":2}],"a_bytes":[1,2,3],"opt_text":"hi","maybe_num":7,"flag":true}',
  ),
  'Foo (populated)',
);
// Sparse: opt_text ABSENT (-> null), maybe_num null, empty map/array/bytes. The empty map and the
// absent optional are what break under the default serde-wasm-bindgen serializer.
check(
  lib.Foo.from_json(
    '{"a_map":{},"a_list":[],"nested":[],"a_bytes":[],"maybe_num":null,"flag":false}',
  ),
  'Foo (sparse: absent optional, null, empties)',
);
// Boundary: maybe_num pinned at the JS safe-integer maximum (2^53-1 = Number.MAX_SAFE_INTEGER).
// At/below this cliff a u64 survives JSON.parse losslessly, so both serializers must still agree
// exactly. This locks the *safe* range; the >2^53 divergence (to_json_value() throws) stays a
// deliberate maintainer call and is intentionally not asserted here.
check(
  lib.Foo.from_json(
    '{"a_map":{},"a_list":[],"nested":[],"a_bytes":[],"maybe_num":9007199254740991,"flag":true}',
  ),
  'Foo (maybe_num at 2^53-1, JS safe-integer max)',
);
// Type choice: externally-tagged enum, serialized via serialize_map (a JS Map under the default
// serializer), so both variants are discriminators.
check(lib.TopChoice.new_uint(5n), 'TopChoice::uint');
check(lib.TopChoice.new_text('hello'), 'TopChoice::text');

console.log('wasm_json roundtrip: to_json_value() matches JSON.parse(to_json()) across all shapes');
