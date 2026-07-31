// LEG 2 of the `component_jco` gate — the payoff leg: TWO independently generated crates, each
// transpiled to its OWN ES module, wired to each other by jco's `--map` and driven through the whole
// cross-crate acceptance flow from JS.
//
// This is the shape `docs/docs/component_differences.mdx` prescribes to a JS consumer, and it is
// prescribed rather than merely described because the obvious alternative — composing the two
// components with `wac` first and transpiling the result — is BROKEN under jco 1.26.1 (pinned by
// `composed.test.mjs`, this file's sibling). So the recommendation is only as good as this file.
//
// The crates come from `tests/component-compose/{dep,consumer}/lib.cddl` — REUSED, never duplicated:
// the same two specs `component_compose` drives through wasmtime. That is the point. Whatever a JS
// host does here, a wasmtime host has already been asked to do with the identical bytes, so a
// disagreement between the two gates is a finding about the HOST rather than about the emitter.
//
// The dependency edge is a plain ES import in the transpiled consumer
// (`import { types } from '../chain/chain.js'`), which is what delivers the instantiate-once
// invariant `component_compose` has to pin by hand: every module importing the same specifier shares
// one instance, by ES module semantics rather than by a composition-time promise.
import { test } from "node:test";
import assert from "node:assert/strict";
import * as chainMod from "../transpiled/chain/chain.js";
import * as walletMod from "../transpiled/wallet/wallet.js";

const chain = chainMod["cddl:chain/types@0.1.0"];
const wallet = walletMod["cddl:wallet/types@0.1.0"];

test("both transpiled modules expose their own interface under its qualified name", () => {
  assert.equal(typeof chain?.Token, "function");
  assert.equal(typeof wallet?.Ledger, "function");
  assert.equal(typeof wallet?.TokenList, "function");
});

test("a dependency handle crosses into a consumer's NON-REPEATED position and comes back live", () => {
  const head = new chain.Token("alice");
  head.setAmount(42n);

  const ledger = new wallet.Ledger(head, new wallet.TokenList(), "memo");
  const back = ledger.head();

  // Values survive the crossing unmutated…
  assert.equal(back.name(), "alice");
  assert.equal(back.amount(), 42n);
  // …and the handle that came back is usable through the DEPENDENCY's own interface, which is the
  // claim the whole cross-crate face exists to make: it is a live resource in chain's table, not an
  // opaque token the consumer minted.
  back.setAmount(99n);
  assert.equal(back.amount(), 99n);
  // Snapshot semantics hold across the seam exactly as they do within one component.
  assert.equal(ledger.head().amount(), 42n);
});

test("the REPEATED position crosses through the accumulator, in both directions", () => {
  const list = new wallet.TokenList();
  list.push(new chain.Token("bob"));
  list.push(new chain.Token("carol"));
  assert.equal(list.len(), 2);

  const ledger = new wallet.Ledger(new chain.Token("alice"), list, "memo");
  assert.deepEqual(
    ledger.entries().map((t) => t.name()),
    ["bob", "carol"],
  );
});

test("an aliasing control: unrelated dependency handles do not perturb a consumer's handles", () => {
  // The exact shape that FAILS on a `wac`-composed artifact (see `composed.test.mjs`): mint several
  // unrelated dependency resources, then read a dependency-typed handle back out of a consumer. On
  // separately transpiled modules the two live in one table and this is uneventful — which is why
  // the gate asserts it here rather than assuming it.
  const ledger = new wallet.Ledger(new chain.Token("alice"), new wallet.TokenList(), "memo");
  const spare = [new chain.Token("zzz1"), new chain.Token("zzz2"), new chain.Token("zzz3")];
  assert.equal(spare.length, 3);

  const head = ledger.head();
  assert.equal(head.name(), "alice");
  assert.equal(head.amount(), undefined);
});

test("the consumer's own CBOR round-trips through the seam", () => {
  const head = new chain.Token("alice");
  head.setAmount(42n);
  const list = new wallet.TokenList();
  list.push(new chain.Token("bob"));

  const ledger = new wallet.Ledger(head, list, "memo");
  const round = wallet.Ledger.fromCborBytes(ledger.toCborBytes());
  assert.equal(round.memo(), "memo");
  assert.equal(round.head().amount(), 42n);
  assert.deepEqual(
    round.entries().map((t) => t.name()),
    ["bob"],
  );
});

test("a dependency handle minted by the CONSUMER's module graph is the same class", () => {
  // The single-instance property, observable: the class the consumer's getter returns is the very
  // class this test file imported from the dependency module, because the transpiled consumer got it
  // by importing that module rather than by instantiating its own copy.
  const ledger = new wallet.Ledger(new chain.Token("alice"), new wallet.TokenList(), "memo");
  assert.ok(ledger.head() instanceof chain.Token);
});
