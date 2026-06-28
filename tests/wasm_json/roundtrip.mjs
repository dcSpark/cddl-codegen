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
// exactly. This locks the *safe* range; the >2^53 contract is pinned separately just below.
check(
  lib.Foo.from_json(
    '{"a_map":{},"a_list":[],"nested":[],"a_bytes":[],"maybe_num":9007199254740991,"flag":true}',
  ),
  'Foo (maybe_num at 2^53-1, JS safe-integer max)',
);
// Above the cliff (u64 > 2^53) the three JSON paths deliberately diverge, and the current behaviour
// is the *blessed* contract (empirically verified; documented in docs/docs/wasm_differences.mdx,
// TESTING_ROADMAP.md item 6):
//   - to_json() (serde_json) stays lossless / full precision;
//   - to_json_value() (serde-wasm-bindgen json_compatible, no bigint) FAILS LOUD — it throws rather
//     than silently handing back a rounded JS number;
//   - JSON.parse(to_json()) is inherently lossy (native JS number) — a JS-engine limit, not our bug.
// Lock the two guarantees that are ours to keep; the lossy JSON.parse value is JS-defined and not
// asserted. A change that makes to_json() lossy, or to_json_value() round silently, fails here.
{
  const big = '9007199254740993'; // 2^53 + 1: first u64 not exactly representable as a JS number
  const foo = lib.Foo.from_json(
    `{"a_map":{},"a_list":[],"nested":[],"a_bytes":[],"maybe_num":${big},"flag":true}`,
  );
  assert.match(
    foo.to_json(),
    new RegExp(`"maybe_num":\\s*${big}\\b`),
    'to_json() must keep a u64 > 2^53 at full precision',
  );
  assert.throws(
    () => foo.to_json_value(),
    /can't be represented as a JavaScript number/,
    'to_json_value() must fail loud (not silently round) for a u64 > 2^53',
  );
  console.log('wasm_json u64 > 2^53: to_json() lossless + to_json_value() fails loud (contract pinned)');
}
// Type choice: externally-tagged enum, serialized via serialize_map (a JS Map under the default
// serializer), so both variants are discriminators.
check(lib.TopChoice.new_uint(5n), 'TopChoice::uint');
check(lib.TopChoice.new_text('hello'), 'TopChoice::text');

console.log('wasm_json roundtrip: to_json_value() matches JSON.parse(to_json()) across all shapes');
