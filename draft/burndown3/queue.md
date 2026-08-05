# Burndown 3 — ordered queue

Effort is a sequencing aid (`S`, `M`, `L`, `XL`), not permission to ship a smaller incorrect
feature. Source links are line anchors at baseline `3df6b3f3`; exact titles remain the stable lookup
key when the files move.

## P0 — current wrongness, silence, panic, or exit-0 broken output

### B3-002 — reject the thirteen inert directives on a spliced group rule

**Status:** BLOCKED — B3-005 parser adoption · **Effort:** S · **Source:** `tests/TESTING_ROADMAP.md` § “On a SPLICED plain
group, the rule slot accepts-and-ignores the thirteen directives” (line 1825).

**Current fact.** At the committed parser revision, every directive on the only observable
single-line spliced-group spelling either has an effect, rejects, or is one of two deliberate no-op
classifications. The separate rule-only slot this item needs exists only after the multi-line parser
adoption in B3-005; taking metadata from the last field now would reject legitimate field metadata.

**Required end state.** After B3-005 adopts the parser slot (or as part of that adoption), add one
rule-position rejection seam for directives the spliced group cannot honor. Reuse the never-spliced
vocabulary/diagnostic structure where possible, while keeping the four meaningful directives as
positive controls.

**Acceptance.** Per-directive vectors prove all thirteen rule-only forms reject once, with a
site-bearing diagnostic; `no_silent_directive` has no allowlist for them; the four meaningful
directives retain their current single-line behavior.

### B3-004 — wasm alias-hop collection cycle panics by rule order

**Status:** READY · **Effort:** M · **Source:** `cddl-matrix/ROADMAP.md` § “A wasm-face alias-hop
cycle ENTERED at the plain-typename rule panics” (line 381).

**Current fact.** Equivalent valid recursive collection specs differ only by topological spelling:
one generates, while the alias-first form aborts under wasm at `IntermediateTypes::is_enum`'s
registered-or-generic assertion. The table sibling was repaired at three structural-dispatch sites;
wasm name derivation is the missed fourth site. The catalog has a tracked `PANIC` fixture.

**Required end state.** Resolve the reference through the collection structural type at the name
derivation seam, with an explicit cycle guard. Equivalent rule orderings must produce the same
support verdict.

**Acceptance.** Flip `recursive_alias_hop_collection_entry.cddl` from `PANIC` to `ok`; generate and
compile default and wasm forms in both orderings; retain the genuine recursive-type boundary tests.

### B3-005 — adopt the built multi-line group trailing-directive parser fix

**Status:** BLOCKED-ACTION (publish fork revision) · **Effort:** M downstream, small upstream action
· **Source:** `tests/TESTING_ROADMAP.md` § “A rule-position directive is still silently LOST in the
multi-line group-rule spelling” (line 1799); burndown2 `T1-09`.

**Current fact.** At pinned `cddl` rev `ac1b98ec`, a trailing directive after a multi-line group is
misbound to the following rule or orphaned. Fork commit
`a7ed0784e89689784ff78ed0e85c7434a3528937` implements an additive `RuleTrailing` fallback; the
codegen reader and vectors were already prepared and verified under a local override. The only
block is making the revision available and bumping the dependency.

**Required end state.** Push/adopt the fork fix and land the prepared codegen side. If publication
cannot happen promptly, investigate whether this spelling can be detected and rejected before it
silently reaches directive handling; do not describe consumer discovery as the next action.

**Acceptance.** Every rule-position directive has equivalent single-/multi-line behavior in
`group_rule_pin_metadata`, the `no_silent_directive` cells run at the committed pin, and no metadata
is double-counted on the single-line form.

### B3-006 — tagged preserve tables must have one wire form

**Status:** DESIGN · **Effort:** L · **Source:** `cddl-matrix/ROADMAP.md` § “A tagged PRESERVE
table's standalone codec drops the tag” (line 197).

**Current fact.** `t = #6.n({* k => v}) ; @duplicates preserve` is a transparent PairMap alias.
Its standalone codec writes/accepts a bare map while every embed site writes/requires the tag. The
exception is named inside `assert_no_wire_facts_survive_a_transparent_alias`; this is a deliberately
bounded but still silent contradiction.

**Required end state.** Prefer a PairMap-aware nominal/synthesized wasm wrapper so the type owns its
tag and the `@newtype` table restriction can retire too. A temporary exact-shape graceful refusal is
acceptable if that vertical implementation cannot land safely in one delivery.

**Acceptance.** Standalone and embedded round trips require the same tag, including byte-exact
preserve behavior. Remove the invariant carve-out; turn the SEAL/flavor-B test carriers positive;
cover Rust, wasm, JSON/WIT/component naming, imports, extern-interface projection, and the matrix
wrapper shape.

### B3-007 — transparent custom-codec aliases must have one codec

**Status:** DESIGN · **Effort:** L · **Source:** `cddl-matrix/ROADMAP.md` § “A transparent alias
carrying a `@custom_serialize`/`@custom_deserialize` PAIR has two wire forms” (line 179).

**Current fact.** Embed sites invoke the named pair, while the emitted transparent alias exposes
the target primitive's standalone `to/from_cbor_bytes`. The docs caution about the discrepancy and
also prescribe the transparent spelling for shapes where wrapping is currently refused. An emitted
public API that disagrees with the declared wire contract is not made safe by lack of known callers.

**Required end state.** Choose and implement one coherent contract: nominalize pair-carrying aliases
so their own impl delegates to the pair, or reject this transparent spelling and provide a truthful
nominal/extern route. The decision must explicitly cover extern/raw-bytes/tag/newtype escape paths
and extern-interface projection.

**Acceptance.** A standalone-vs-embedded e2e test proves identical custom wire behavior under
default and preserve profiles. No `AliasInfo` metadata can change embed behavior without changing
or suppressing the alias's standalone API; docs show a supported migration.

### B3-031 — make accepted named-struct custom codec pairs symmetric

**Status:** DESIGN · **Effort:** M–L · **Source:** `tests/TESTING_ROADMAP.md` line 2354.

**Current fact.** A named record carrying both `@custom_serialize` and `@custom_deserialize` is
accepted. The generated side suppresses ordinary impls and routes embed-site reads to the custom
reader, but it never references the custom writer. This is silent one-sided custom wire behavior,
not the truthful whole-table refusal owned by `B3-026`.

**Required end state.** Generate thin `Serialize` and `Deserialize` impls that delegate to the two
named functions for every accepted named struct kind, including the missing serialize-side
`Root(Rust(ident))` arm. If a struct kind cannot honor both halves, reject the pair on that kind
before emission.

**Acceptance.** Direct and embedded serialization/deserialization call the same named pair;
default/preserve round trips prove one wire contract; extern-interface projection remains opaque
and sound; the old accepted-asymmetric control cell becomes a positive symmetric-delegation cell.

### B3-027 — finish shared-runtime cross-flavor accommodation

**Status:** READY · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` line 2407.

Two accepted shared-runtime configurations generate consumers that fail E0277/E0599: a preserve
runtime lacks the `TryFrom<BTreeMap>` bridge a reduced `{+ K => V}` consumer calls, and a canonical
runtime lacks the one-argument `AnyCbor::serialize` a reduced `any` consumer calls. Add both
compatibility shims, invert the existing config mutation proofs, and compile reduced-flavor
consumers. Do not alter the maintainer-closed `[runtime]` carrier derivation; this repairs runtime
compatibility within the already-selected carrier.

## P1 — current public-surface defect and due coverage systems

### B3-008 — keep json2ts declaration names keyed to `$defs`

**Status:** DESIGN · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` § “Guarantee `<$defs
key>JSON` is the emitted declaration name” (line 2645).

`json-schema-to-typescript` normalizes titles (`Blake2b256` → `Blake2B256JSON`), so
`json-ts-types.js` cannot associate a published class with its interface and the shipped `.d.ts`
silently falls back to `any`. Use collision-proof normalization-fixed synthetic titles, compile,
then map exact declarations back to `<key>JSON` without rewriting comments or string unions.
Acceptance is an awkward-name fixture whose final merged declaration stays typed, plus collision,
comment, and string-literal controls.

### B3-009 — classify fixed-value arm rejection with real vectors

**Status:** READY · **Effort:** M · **Source:** `cddl-matrix/ROADMAP.md` § “The fixed-value ARM
position has no enforcement classification” (line 48).

Author wrong-arm/no-arm vectors now; do not wait for one to appear incidentally. Classify
group-choice and choice-member rejection according to dispatch semantics, and map-key rejection as
required-member lookup rather than equality. Extend `cellCarriesConstraint` only after the vectors
exist. Q4 must then fail if another supported fixed arm lands without rejection evidence.

### B3-010 — add the spec-valid/policy-rejected decode vector class

**Status:** READY · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` § “Mint
decode-conformance + confirm matrix coverage for the tag-258 reject-default flip” (line 120).

The tag-258 default rejects duplicate elements by library policy although the underlying `[* T]`
CDDL is valid; neither `constraint` nor `over-acceptance` can represent that evidence. Add a
`policy-rejected` class through catalog schema, mint, oracle checks (both must accept), replay,
projection, counts, and stale guards. Then pin duplicate tag-258 bytes to the generated decoder's
`DuplicateKey` door.

### B3-011 — compile extern corpus/matrix cells at breadth

**Status:** READY · **Effort:** M–L · **Source:** `cddl-matrix/ROADMAP.md` § “Extern compile
coverage at BREADTH” (line 673).

Reuse the raw-bytes def-splice pattern to supply trivial extern definitions and root re-exports, so
extern cells compile Rust, wasm, and json-gen rather than remaining blanket-exempt. Enumerate generic
extern bases with/without instances first; make generic raw-bytes a matrix reject row if it remains
an invalid shape. Remove exemptions only when their stale guards and emitted-crate checks are live.

### B3-012 — directly test the two remaining write-tail routes

**Status:** READY · **Effort:** S · **Source:** `tests/TESTING_ROADMAP.md` item 1 (line 146).

Add `WriteTailPlan` cases for `composed_runtime_files` and `static_crate`. Pin preservation and
fixed-point behavior for the per-file runtime route; pin hand-owned target writes, manifest merge,
and the existence-gated new-static-file notice for the export-static route. These are direct tempdir
tests: no CDDL or nested cargo is needed.

### B3-013 — enumerate float fixed values in arm and member positions

**Status:** READY · **Effort:** S–M · **Source:** `cddl-matrix/ROADMAP.md` § “Enumerate the FLOAT
fixed-value kind” (line 428).

The preserve float blocker is gone and `optional_fixed_float.cddl` is a precedent. Add member and
arm cells, spelling whether the arm takes type-match or same-major brute-force dispatch; include
accept and rejection evidence and update projections/Q4. This is the clearest small matrix win.

### B3-014 — add tagged-optional preserve to recombination

**Status:** READY · **Effort:** S · **Source:** `tests/TESTING_ROADMAP.md` § “The recombination
member-kind table does not span the tagged-optional shape” (line 1840).

Add a named `T / null` under a tag to the ingredient table and require the preserve layer-2 crate to
compile. The missing composition already allowed an exit-0 E0308 escape; a second incident is not
needed to justify one cheap systematic row.

### B3-015 — enumerate root-reference placement in the multifile matrix

**Status:** READY · **Effort:** S–M · **Source:** `cddl-matrix/ROADMAP.md` § “Enumerate the
multifile matrix's referencing-MODULE axis” (line 812).

Add a `rootref` mode for wrapper-minting shapes with the same participation pins as `anon`. Exercise
the distinct root/no-import path and keep scope/module controls. Run the matrix projection check and
its compile/round-trip floor.

### B3-016 — cover directory input for flag-gated emission surfaces

**Status:** READY · **Effort:** M · **Source:** `cddl-matrix/ROADMAP.md` § “Third honesty axis —
flag-gated EMISSION SURFACES × input mode” (line 857).

State and test the directory posture of `--emit-tests-conformance`, wasm macro modes (compile-only),
and execution of json-gen over cross-module types. Keep existing deliberate single-file postures
explicit. This closes the same systematic hole that previously let every multifile `--emit-tests`
crate fail E0433.

### B3-017 — run the robustness catalog out of process

**Status:** READY · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` item 12 (line 482).

Use the existing child entry point for every robustness input instead of trusting the hand-maintained
`ABORT_PRONE_INPUTS` subset. A newly introduced non-unwinding abort should become a classified row,
not kill the test binary. Measure the local-tier process-launch cost and preserve the catalog's
current outcome semantics.

### B3-018 — add an `annotate-fields=false` feature-corpus profile

**Status:** READY · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` § “Full `2^N` flag
powerset / PICT pairwise” residual (line 3135).

Two shape-poor smoke escapes have already demonstrated that one canonical input cannot exercise the
flag's divergent deserialize paths. Add a cached corpus or deserialize-subset profile under
`--annotate-fields=false`; do not build the full flag powerset. The profile must include fixed
members and bounded nint shapes that caused the known failures.

### B3-019 — type-check every shipped TypeScript boundary, with shared setup

**Status:** READY · **Effort:** M–L · **Source:** `tests/TESTING_ROADMAP.md` lines 2598 and 2612.

Centralize one lock-protected/shared npm installation and run the existing real-schema projection
check over every `--json-schema-export` fixture rather than the current two callers. Then type-check
the final wasm-pack-plus-JSON merged `.d.ts` in `package_json_pipeline` with `tsc --noEmit`, no
`skipLibCheck`, TypeScript ≥5.2, and `--target esnext`. Keep this distinct from `B3-008`: this card
builds the public-output oracle; `B3-008` repairs a known naming defect the oracle must exercise.

### B3-020 — remove current generated unused imports, then broaden warning scans

**Status:** READY · **Effort:** S–M · **Source:** `tests/TESTING_ROADMAP.md` § “`unused_imports` on
generated crates” (line 1273).

Fix the known multifile `src/generated/mod.rs` `use serialization::*;` warning first. Then restrict
the extern-deps `run_test` captures to warnings originating in generated files and reuse the live
unused-import/unused-variable scanner there. Do not combine this with the riskier method-call-aware
trait-import pruning: the glob is a concrete emission defect, while the trait residue needs a
separate conservative-keep design and compile breadth.

### B3-032 — make `lint_doc_citations` report missing tracked files cleanly

**Status:** READY · **Effort:** S · **Source:** `tests/TESTING_ROADMAP.md` operational watch at
line 2752.

`readText(): string | null` currently calls `readFileSync` without handling ENOENT, so an ordinary
tracked-but-unstaged fixture move produces a Bun stack trace rather than a citation verdict. Catch
the missing-file case, name the path and staged-state remedy, and add a self-test with a tracked path
absent from the working tree. Decide glob citations separately; they are not required to close the
crash.

### B3-033 — execute representative component features, not only compile them

**Status:** DESIGN · **Effort:** L · **Source:** `cddl-matrix/ROADMAP.md` § “A component-face
emission leg” (line 117).

Build a bounded sibling to the wasm execution probe: generate representative per-feature component
crates, drive round trips under a host, and carry component generation/mint/round-trip stages in the
evidence. Start with features whose WIT projection despecializes or validates values and the
component-specific defect families already found. Compilation breadth remains owned by
`component_corpus_compiles`; this card must detect glue that builds and behaves incorrectly.

## P2 — coherent feature programs (valid CDDL or accepted cross-crate mode)

### B3-021 — real incremental `/=` and `//=` choice extension

**Status:** DESIGN · **Effort:** L · **Source:** `cddl-matrix/ROADMAP.md` line 282.

Merge extensions before ordinary choice parsing while preserving statement-local arm metadata,
comment-parent identity, deterministic order, and a defined cross-module owner. Flip the two reject
rows only with default/preserve/wasm and cross-scope decode coverage.

### B3-022 — one occurrence/bounds program for arrays and tables

**Status:** DESIGN · **Effort:** XL · **Sources:** `cddl-matrix/ROADMAP.md` lines 296, 311, 329,
and 511; `tests/TESTING_ROADMAP.md` line 2496.

Unify non-final optional/repeated array decoding with bounded containers: use definite length plus
major-type peek, define genuine ambiguity/indefinite policy, and introduce sibling
`BoundedVec`/`BoundedMap` representations with a single checked conversion door. Include bounded
tables, exact-once arrow entries, and the open-table typed-row bypass. Land vertical phases with
boundary accept/reject vectors; do not retain a bare mutable container while claiming type-level
enforcement.

### B3-023 — support legal same-chain nested `.cbor`

**Status:** DESIGN · **Effort:** M · **Source:** `cddl-matrix/ROADMAP.md` line 527.

Thread `cbor_depth` beside `tag_depth` through serializer, deserializer, and preserve sidecar names;
parenthesize the recombination builder so legal self-compositions are visible. Flip the narrow
refusal only when default/preserve code compiles and hand-derived nested bytes round-trip.

### B3-024 — complete fixed-value representations

**Status:** DESIGN · **Effort:** XL · **Sources:** `cddl-matrix/ROADMAP.md` lines 398, 451, 468,
and 490; `tests/TESTING_ROADMAP.md` line 2525.

Build a zero-size/presence representation for standalone fixed rules and fixed-or-null, add
`Undefined` and byte-string fixed values, and coordinate the upstream lowercase-hex/base64 parser
repair. Treat expected-conversion tags over `any` as a separate tagged-Any phase with an explicit
API. Every fixed value must validate on decode and re-emit the declared constant across generated
faces.

Close it as independent vertical phases while every unimplemented row stays a loud refusal:

1. `B3-024A`: top-level scalar/text/bool/null literals and fixed-or-null presence.
2. `B3-024B`: `undefined` as a real fixed value.
3. `B3-024C`: byte-string literals plus the upstream lowercase-hex/base64 parser work.
4. `B3-024D`: expected-conversion tags over `any`, only after its representation/API decision.

### B3-025 — preserve enum-level tags on anonymous choices

**Status:** DESIGN · **Effort:** L · **Source:** `cddl-matrix/ROADMAP.md` line 646.

Give the enum rule its own tag encoding metadata rather than forcing the fact into a variant
sidecar. Prove non-minimal tag-head preservation for type-choice, group-choice, and all-fixed forms;
retain named-wrapper controls and retire the preserve skip/refusal together.

### B3-026 — give a whole-table custom codec pair one truthful owner

**Status:** DESIGN · **Effort:** L · **Sources:** `tests/TESTING_ROADMAP.md` line 2370;
`cddl-matrix/ROADMAP.md` line 618.

Keep the current table-rule refusal until a wrapper makes the whole map—not its individual keys and
values—the codec owner. Then thread its metadata through the `AliasInfo` consumers, docs, alias
suppression, and extern-interface projection. Share the ownership model with `B3-007` and `B3-031`,
but close this card independently; no accepted table spelling is currently silent.

### B3-028 — auto-defer reject-set wrappers through wrapper requests

**Status:** DESIGN · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` line 2431.

Route `generate_reject_ordered_set_type` through `try_defer_wrapper`, thread the three placement call
sites, write the consumer sidecar automatically, and link a real multi-crate fixture. The current
duplicate-symbol failure is loud, but it is still a partial cross-crate feature, and tag-258 makes
the affected audience larger.

### B3-029 — settle component `cdylib` output and add the wasip2 workspace gate

**Status:** DESIGN · **Effort:** M · **Source:** `tests/TESTING_ROADMAP.md` line 2318.

Decide from the product contract whether `--component` needs the incidental rust `cdylib`; drop or
feature-gate it if not. Then land the red-first workspace-mode `wasm32-wasip2` fixture that currently
reproduces the linker SIGSEGV. “Changing generated manifests” is not a reason to leave a documented
crash path unresolved.

## Completed

Compact format only: `B3-NNN — DONE (<commit>); source entry retired/rewritten; <tier> PASS
(<wall time>)`. Put design conclusions in permanent docs/tests and implementation detail in the
commit, not in a growing narrative here.

- B3-001 — DONE (`eda97688`; audit `255f2ec9`); source entry retired; local PASS (11m 22s).
- B3-003 — DONE (`f2b49561`); source entry retired; local PASS (9m 32s).
