// LEG 1 of the `component_jco` gate: what a JS host actually SEES when it transpiles ONE generated
// component with jco and calls it.
//
// Every assertion here is a claim `docs/docs/component_differences.mdx` makes to a JS consumer. They
// are runtime observations rather than readings of the emitted `.d.ts`, because a `.d.ts` describes
// what jco INTENDS and this gate's subject is what the boundary DOES — the two disagreed on the enum
// face (typed as string labels, and a NUMBER is rejected at run time, which is the single most
// likely port break for a consumer coming from the wasm-bindgen face).
//
// The component is `tests/component-host/inputs/lib.cddl` — REUSED, never duplicated. That fixture
// was designed to carry one shape per assertion class (see its header), and every class below is one
// of them seen from JS instead of from wasmtime.
//
// No test-framework dependency: node's built-in runner plus `node:assert`. The only npm dependencies
// this gate has are jco and its wasi shim.
import { test } from "node:test";
import assert from "node:assert/strict";
import * as mod from "../transpiled/host/host.js";

// The FULLY-QUALIFIED interface name, never the bare `types` alias. On a single-component artifact
// both resolve; on a composed one the bare alias silently picks the first interface only (the
// duplicate-`export * as types` defect this gate's third leg pins), so the qualified spelling is the
// one the docs teach and therefore the one the gate exercises.
const T = mod["cddl:cddl-lib/types@0.1.0"];

const digest = () => new T.Hash(new Uint8Array(32).fill(3));

// The 10 mandatory `record` fields, in declaration order.
const record = () =>
  new T.Record(
    1n,
    "name",
    digest(),
    { tag: "uint", val: 5n },
    ["a"],
    [["k", 1n]],
    "i1",
    T.Value.newText("hi"),
    new T.Node("root"),
    new Uint8Array([0x01]),
  );

test("the interface is reachable under its fully-qualified name", () => {
  assert.ok(T, "the transpiled module exports no `cddl:cddl-lib/types@0.1.0`");
  for (const cls of ["Hash", "Record", "Node", "Value"]) {
    assert.equal(typeof T[cls], "function", `${cls} is not a class on the interface`);
  }
});

test("a WIT enum is a STRING label, and a number is rejected", () => {
  assert.equal(record().kind(), "i1");
  assert.equal(typeof record().kind(), "string");
  // The port break: the wasm-bindgen face takes the numeric discriminant, this one refuses it.
  assert.throws(
    () =>
      new T.Record(
        1n,
        "n",
        digest(),
        { tag: "uint", val: 5n },
        ["a"],
        [["k", 1n]],
        1,
        T.Value.newText("h"),
        new T.Node("r"),
        new Uint8Array([1]),
      ),
    (e) => e instanceof TypeError && /not one of the cases of color/.test(e.message),
  );
});

test("a free function lands beside the classes as a module-level export", () => {
  // 0x63 'a' 'b' 'c' — a definite-length text string.
  assert.equal(T.cborKind(new Uint8Array([0x63, 0x61, 0x62, 0x63])), "text");
});

test("`variant int` crosses as {tag, val}, both signs", () => {
  assert.deepEqual(record().delta(), { tag: "uint", val: 5n });
  const negative = new T.Record(
    1n,
    "n",
    digest(),
    { tag: "nint", val: 4n },
    ["a"],
    [["k", 1n]],
    "i0",
    T.Value.newText("h"),
    new T.Node("r"),
    new Uint8Array([1]),
  );
  // `nint` carries the -(n+1) bias unapplied, exactly as the WIT variant declares it.
  assert.deepEqual(negative.delta(), { tag: "nint", val: 4n });
});

test("`option<option<T>>` really has three distinguishable states", () => {
  // absent
  assert.deepEqual(record().nested(), { tag: "none" });
  // present, text
  const present = record();
  present.setNested("x");
  assert.deepEqual(present.nested(), { tag: "some", val: "x" });
  // present, null — `some` with NO `val` key, which is how the inner option's flattening surfaces.
  const explicitNull = record();
  explicitNull.setNested(undefined);
  const got = explicitNull.nested();
  assert.equal(got.tag, "some");
  assert.ok(!("val" in got) || got.val === undefined, `expected a val-less some, got ${JSON.stringify(got)}`);
});

test("`any` crosses as Uint8Array in both directions, and trailing data is refused", () => {
  const meta = record().meta();
  assert.ok(meta instanceof Uint8Array);
  assert.deepEqual(Array.from(meta), [0x01]);

  const withExtra = record();
  withExtra.setExtra(new Uint8Array([0x18, 0x2a])); // one CBOR item: uint 42
  assert.deepEqual(Array.from(withExtra.extra()), [0x18, 0x2a]);

  // TWO items in one `any` field is not one CBOR value, and the boundary says so rather than
  // silently keeping the first.
  assert.throws(
    () => record().setExtra(new Uint8Array([0x01, 0x02])),
    (e) => /trailing data/i.test(String(e.message ?? e)),
  );
});

test("a failing door THROWS a ComponentError carrying the WIT string as .payload", () => {
  let thrown;
  try {
    new T.Hash(new Uint8Array(3));
    assert.fail("a 3-byte hash was accepted");
  } catch (e) {
    thrown = e;
  }
  assert.ok(thrown instanceof Error, "the thrown value is not an Error");
  assert.equal(thrown.constructor.name, "ComponentError");
  assert.equal(typeof thrown.payload, "string");
  assert.match(thrown.payload, /3 not in range 32 - 32/);
  assert.equal(thrown.message, thrown.payload);
});

test("a failed fallible constructor does not poison the component", () => {
  assert.throws(() => new T.Hash(new Uint8Array(3)));
  // The whole reason the emitter returns `result<…>` instead of trapping: a trap would take the
  // instance down with it and every later caller would die on a dead component.
  assert.equal(new T.Hash(new Uint8Array(32)).get().length, 32);
  assert.equal(record().name(), "name");
});

test("a despecialized NonEmpty invariant is re-imposed at the consuming door", () => {
  // `names = [+ text]` is a plain `list<string>` in WIT, so the bound lives on the setter.
  assert.throws(
    () => record().setAliases([]),
    (e) => /0 not at least 1/.test(String(e.message ?? e)),
  );
  const ok = record();
  ok.setAliases(["x"]);
  assert.deepEqual(ok.aliases(), ["x"]);
});

test("lending the same handle as receiver AND argument returns normally and stores a snapshot", () => {
  const node = new T.Node("a");
  node.setChildren([node]);
  assert.deepEqual(
    node.children().map((c) => c.label()),
    ["a"],
  );
  // Not an aliased cycle: the stored child is a copy, so the instance is still usable afterwards.
  assert.equal(node.label(), "a");
});

test("a getter hands back a snapshot, not an alias", () => {
  const grandchild = new T.Node("g");
  const child = new T.Node("c");
  child.setChildren([grandchild]);
  const parent = new T.Node("a");
  parent.setChildren([child]);

  const got = parent.children();
  assert.equal(got.length, 1);
  assert.deepEqual(
    got[0].children().map((n) => n.label()),
    ["g"],
  );
  // Mutating the handle the getter returned must not reach the parent: it is a fresh resource
  // deserialized from the parent's bytes, not a view into the parent's own storage.
  got[0].setChildren([]);
  assert.deepEqual(got[0].children(), []);
  assert.deepEqual(
    parent.children()[0].children().map((n) => n.label()),
    ["g"],
  );
});

test("a type choice surfaces as new-<variant> statics plus a kind discriminant", () => {
  const v = T.Value.newUint(9n);
  assert.equal(v.kind(), "uint");
  assert.equal(v.asUint(), 9n);
  assert.equal(v.asText(), undefined);
});

test("[Symbol.dispose] is an OWN property of each instance, and disposal is observable", () => {
  const node = new T.Node("a");
  // The practical instruction for a consumer: feature-detect on the INSTANCE. jco does not put it on
  // the prototype, so `Cls.prototype[Symbol.dispose]` is a false negative.
  assert.equal(typeof T.Node.prototype[Symbol.dispose], "undefined");
  assert.equal(typeof node[Symbol.dispose], "function");
  assert.ok(Object.getOwnPropertySymbols(node).includes(Symbol.dispose));

  node[Symbol.dispose]();
  assert.throws(
    () => node.label(),
    (e) => e instanceof TypeError && /not a valid "node" resource/i.test(e.message),
  );
});

test("the boundary round-trips its own CBOR bytes", () => {
  const r = record();
  const bytes = r.toCborBytes();
  assert.ok(bytes instanceof Uint8Array);
  assert.equal(T.Record.fromCborBytes(bytes).name(), "name");
});
