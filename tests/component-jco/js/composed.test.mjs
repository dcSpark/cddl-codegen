// LEG 3 of the `component_jco` gate — a KNOWN-BROKEN PIN, and the only leg here whose GREEN means
// "still broken".
//
// `component_compose` composes these same two generated components into one dual-export world with a
// composer and drives it through wasmtime, correctly. Transpile that SAME composed artifact with jco
// 1.26.1 and the cross-instance resource story falls apart: jco allocates a separate handle table per
// component INSTANCE for the same resource type and emits no transfer between them for a JS-held
// handle. That is a jco defect, not an emitter one — the artifact is the one wasmtime accepts.
//
// It is pinned rather than merely written down because its worse half is SILENT: one symptom throws,
// the other hands back a different object and lets the program continue. A consumer who reads only
// the doc paragraph and tries it anyway gets a wrong answer, not an error.
//
// WHEN THIS FILE FAILS, THAT IS GOOD NEWS. See the failure messages: each one says what to do.
//
// The composition is the same shape the docs describe: `wac compose` over the two built components
// with the consumer's dependency import satisfied by the dependency instance's export. The `wac`
// binary is ambient (it is the only ambient-binary dependency this gate has), so the Rust gate
// preflights it and runs this file ONLY when it is present at 0.9.0 or newer — the other two legs
// never need it.
import { test } from "node:test";
import assert from "node:assert/strict";
import * as world from "../transpiled/composed/composed.js";

const chain = world["cddl:chain/types@0.1.0"];
const wallet = world["cddl:wallet/types@0.1.0"];

// The good-news text, shared by every assertion below so a fix reads identically wherever it lands.
const FIXED =
  "jco now handles cross-instance resource tables — `component_differences.mdx` must stop telling " +
  "consumers to avoid `wac`-composed artifacts, and this pin must be replaced by a positive assertion.";

// `[["alice", 42], [], "memo"]` — a ledger whose head is alice/42, built from bytes so no chain
// handle is minted on the way in.
const LEDGER_BYTES = new Uint8Array([
  0x83, 0x82, 0x65, 0x61, 0x6c, 0x69, 0x63, 0x65, 0x18, 0x2a, 0x80, 0x64, 0x6d, 0x65, 0x6d, 0x6f,
]);

test("the composed world still exports both interfaces under their qualified names", () => {
  // Not part of the defect: the artifact itself is well-formed, which is precisely why the failure
  // below is a surprise to a consumer rather than an obvious mistake.
  assert.equal(typeof chain?.Token, "function");
  assert.equal(typeof wallet?.Ledger, "function");
  // The secondary defect on the same artifact: BOTH packages' single interface is named `types`, so
  // the root `.d.ts` emits `export * as types` twice (invalid TypeScript) and the bare alias
  // resolves to the FIRST interface only. Pinned so the docs' "always use the qualified name"
  // instruction has an owner.
  assert.notEqual(world.types, wallet, "the bare `types` alias no longer aliases only chain — recheck the duplicate-export note in `component_differences.mdx`");
});

// FIRST in the file on purpose: the symptom is that unrelated chain-handle allocation perturbs the
// lookup, so this must observe a process in which nothing has minted one yet. `node --test` runs each
// file in its own child process, so file order does not matter but IN-FILE order does.
test("PIN: a consumer getter silently returns the WRONG dependency object once handles have been minted", () => {
  const clean = wallet.Ledger.fromCborBytes(LEDGER_BYTES).head();
  assert.equal(clean.name(), "alice", "the composed artifact's first read is already wrong — a different defect from the pinned one");
  assert.equal(clean.amount(), 42n);

  // Three unrelated dependency resources, doing nothing to the ledger.
  const spare = [new chain.Token("zzz1"), new chain.Token("zzz2"), new chain.Token("zzz3")];
  assert.equal(spare.length, 3);

  const after = wallet.Ledger.fromCborBytes(LEDGER_BYTES).head();
  assert.notEqual(
    after.name(),
    "alice",
    `the wrong-object symptom is GONE: a composed artifact's dependency-typed getter now returns the right resource. ${FIXED}`,
  );
});

test("PIN: a dependency handle cannot be passed into a consumer constructor (scalar borrow)", () => {
  const head = new chain.Token("alice");
  assert.throws(
    () => new wallet.Ledger(head, new wallet.TokenList(), "memo"),
    (e) => e instanceof TypeError && /not a valid "token" resource/i.test(e.message),
    `passing a composed artifact's dependency handle into a consumer constructor now WORKS. ${FIXED}`,
  );
});

test("PIN: a dependency handle cannot be pushed through a consumer accumulator (repeated position)", () => {
  const list = new wallet.TokenList();
  assert.throws(
    () => list.push(new chain.Token("bob")),
    (e) => e instanceof TypeError && /not a valid "token" resource/i.test(e.message),
    `pushing a composed artifact's dependency handle through a consumer accumulator now WORKS. ${FIXED}`,
  );
});
