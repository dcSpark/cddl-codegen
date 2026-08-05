# Burndown 3 — first-principles queue

This is the third managed pass over `cddl-matrix/ROADMAP.md` and
`tests/TESTING_ROADMAP.md`. It starts from clean `master` at `3df6b3f3` (2026-08-05).
The audit read both roadmaps in full, checked the highest-priority claims against current code and
tests, and used independent matrix, testing, and cross-cutting reviews. No long gate was run: this
delivery changes planning documents only.

Burndown 2 proved that waiting for a consumer is a poor discriminator. Its remaining board is
mostly honest limitations, but the standing roadmaps have since accumulated new cases where the
tool succeeds while producing wrong bytes, silently discarding a directive, panicking, or emitting
a crate that cannot compile. Those are current defects even when their only reproducer is synthetic.

## Rules for this pass

1. An accepted spelling has one coherent meaning on every generated entry point. Documentation
   cannot make two wire formats for one type acceptable.
2. Exit 0 means every emitted crate in the requested face builds. If full support is not ready, a
   targeted, graceful refusal is a valid short-term repair; exit-0 broken output is not.
3. A directive is either honored or rejected. Accepted-and-inert is always a defect.
4. Equivalent valid CDDL must not change from success to panic because of rule ordering.
5. Breaking generated APIs or newly rejecting a previously accepted broken spelling is allowed
   when that is the shortest path to a truthful contract.
6. Graceful refusals are still feature work, but they follow silent wrongness and honesty failures.
   “No consumer yet” affects sequencing, not whether the item belongs on the board.
7. Every item is re-probed at pickup. A roadmap narrative is a claim, not evidence; burndown 2
   repeatedly found stale or inverted premises.

## Files

- `queue.md` is the ordered execution board. Its first tranche is deliberately small enough to
  start immediately; later sections group coherent feature and test-system programs.
- `source-dispositions.md` records stale entries, non-work ledgers, and the review policy for the
  large recur-first section. It prevents shrinking the source files by merely copying every watch
  into this folder.

The two source roadmaps remain the detailed evidence ledgers until an item is delivered. A delivery
must update or remove its source entry in the same change; burndown3 is not a third permanent home
for completed history.

## Execution order

### Tranche A — narrow honesty fixes

Run these in order unless code overlap makes a combined delivery safer:

1. `B3-003` — requested-hosted NonEmpty co-host self-import E0432.
2. `B3-004` — wasm alias-hop collection panic.
3. `B3-005` — publish/adopt the already-built multi-line group trailing-comment parser fix.
4. `B3-002` — rule-only directive rejection after B3-005 exposes the slot; the committed parser
   has no separate slot, so it must not repurpose last-field metadata.

The first three have a current in-tree reproducer, a narrow implementation seam, and an unambiguous
acceptance condition. `B3-005` is coordination rather than research; it stays in this tranche
because the current behavior silently loses directives.

### Tranche B — contradictory public contracts

5. `B3-006` — tagged preserve table has two wire forms.
6. `B3-007` — transparent custom-codec alias has two wire forms.
7. `B3-031` — accepted named-struct custom codec pairs ignore the writer.
8. `B3-027` — accepted shared-runtime flavor combinations emit crates that do not compile.
9. `B3-008` — json2ts normalization silently removes a published type from the typed wasm surface.

These need explicit design work, but not a decision about whether correctness matters. For
`B3-006` and `B3-007`, a precise graceful refusal can land before the full nominal representation
if that is the only safe bounded change. It must replace the broken acceptance, not coexist with it.

### Tranche C — make present support claims self-checking

`B3-009` through `B3-020`, plus `B3-032` and `B3-033`, close known blind spots with existing
machinery: fixed-arm rejection
evidence, the tag-258 policy vector class, extern compilation, direct write-tail cases, the two
buildable float cells, tagged-optional recombination, root-reference placement, directory-mode
flag surfaces, out-of-process robustness, the `annotate-fields=false` corpus profile, TypeScript
output checks, generated unused-import cleanup, citation-lint failure reporting, and component
execution coverage.

### Tranche D — first-principles feature programs

`B3-021` through `B3-029` (except promoted `B3-027`) turn valid-CDDL refusals and incomplete
cross-surface features into coherent programs rather than one-off patches. Each program must land vertically—IR,
serialization/deserialization, Rust/WASM/JSON/WIT where applicable, matrix classification, and
spec-derived boundary vectors—before its reject rows flip to supported.

### Maintainer-scale work

The complete `cargo-mutants` sweep is permanently declined. It must never be scheduled, added to a
later burndown, or given a reopening signal; the partial experiment remains historical evidence
only. The formatter replacement also remains a recorded decision, not an active item: the
repository's emitted-token stability and overlay contract are real engineering constraints, and
the current AGENTS ruling says to retain string emission/rustfmt workarounds absent new evidence.
The two already-proven rustfmt exposures are such evidence for future reconsideration, but this
planning pass does not silently override that explicit ruling.

## Item lifecycle

Each queue item has a status: `READY`, `BLOCKED-ACTION`, or `DESIGN`. On pickup, change
it to `IN-PROGRESS (<owner/context>)` and record the fresh reproduction SHA. On delivery, remove the
full card from `queue.md`, add a one-line result under `queue.md`'s `Completed` section, and prune or
rewrite the source-roadmap entry. Do not preserve long delivery narratives in this folder; git
history and the permanent test/doc contract are the durable record.

At most four items should be in progress. A large program may be split into reviewed vertical
phases, but a phase may not claim support while another requested face still silently disagrees.

## Verification policy

Planning changes use link/identifier checks only. Implementation deliveries follow the repository
gate policy:

- run the smallest red-first unit/integration gate while developing;
- run `bun run check.ts` before considering a delivery complete;
- run `bun run check.ts full` for a feature or a change to matrix/decode/component breadth;
- keep multi-minute output in `draft/logs/`, with the conclusion and timing copied into any durable
  record rather than citing the disposable path;
- never regenerate a real downstream consumer to validate a fix.
