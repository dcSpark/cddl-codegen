// JS oracle for the `serde_json/arbitrary_precision` regression: runs the wasm-pack-built bindings
// under node and asserts what only a JS engine can see — that `to_json_value()` hands back real JS
// numbers rather than the private `$serde_json::private::Number` token objects serde-wasm-bindgen
// emits verbatim when serde_json's number impl is dishonest.
//
// Two assertions per value, because either alone is weak. The deep-equal against
// `JSON.parse(to_json())` is the same invariant `tests/wasm_json/roundtrip.mjs` pins, but it would
// pass if BOTH sides were equally wrong; the token-absent check is the positive half that cannot.
//
// argv[2] is the wasm-pack `pkg` dir; requiring the directory resolves the right module via the
// package.json "main" it generates, so this stays agnostic to the lib name.
import assert from 'node:assert';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const lib = require(process.argv[2]);

const TOKEN = '$serde_json::private::Number';

function check(obj, label) {
  const value = obj.to_json_value();
  assert.deepStrictEqual(
    value,
    JSON.parse(obj.to_json()),
    `${label}: to_json_value() != JSON.parse(to_json())`,
  );
  assert.ok(
    !JSON.stringify(value).includes(TOKEN),
    `${label}: to_json_value() carries the private ${TOKEN} token: ${JSON.stringify(value)}`,
  );
}

// The half we own: an `any`-carrying member, whose serde adapter builds a serde_json::Value.
// Every magnitude at or below the JS safe-integer cliff must agree exactly.
for (const n of ['0', '1', '1000', '9007199254740991']) {
  check(lib.AnyHolder.from_json(`{"payload":${n}}`), `AnyHolder(any=${n})`);
}
// Negative `any` values take the signed arm of the ladder.
for (const n of ['-1', '-3', '-9007199254740991']) {
  check(lib.AnyHolder.from_json(`{"payload":${n}}`), `AnyHolder(any=${n})`);
}
// The consumer-shaped half: a `@custom_json` newtype whose hand-written Serialize routes a
// serde_json::Value through the shipped helper.
for (const n of ['0', '1000', '9007199254740991']) {
  check(lib.CustomNum.from_json(`{"int":${n}}`), `CustomNum(${n})`);
}

// Above the JS safe-integer cliff the blessed contract (docs/docs/wasm_differences.mdx, pinned by
// `wasm_json_roundtrip`) is: to_json() stays lossless, to_json_value() FAILS LOUD rather than
// silently rounding. An honest number impl is what makes a big integer REACH that refusal at all —
// a dishonest one bypasses it entirely and hands back a lossless-but-unusable token object, which is
// strictly worse than the loud failure. So this asserts the fix strengthens the >2^53 contract
// rather than bending it.
{
  const big = '9007199254740993'; // 2^53 + 1: first u64 not exactly representable as a JS number
  const holder = lib.AnyHolder.from_json(`{"payload":${big}}`);
  assert.match(
    holder.to_json(),
    new RegExp(`"payload":\\s*${big}\\b`),
    'to_json() must keep an `any` uint > 2^53 at full precision',
  );
  assert.throws(
    () => holder.to_json_value(),
    /can't be represented as a JavaScript number/,
    'to_json_value() must fail loud (not silently round, and not hand back a token) for > 2^53',
  );
  const custom = lib.CustomNum.from_json(`{"int":${big}}`);
  assert.throws(
    () => custom.to_json_value(),
    /can't be represented as a JavaScript number/,
    'the same loud refusal must reach a hand-written Serialize routed through the helper',
  );
  console.log('json-arbitrary-precision: > 2^53 still fails loud on both surfaces (contract pinned)');
}

console.log(
  'json-arbitrary-precision: to_json_value() returns real numbers, no private token, on both the `any` and `@custom_json` surfaces',
);
